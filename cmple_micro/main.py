from deta import Deta
from fastapi import FastAPI, UploadFile, Form
from fastapi.responses import Response
from pydantic import BaseModel, validator
import json
import re

with open("credentials.json") as f:
    credentials = json.load(f)


class Item(BaseModel):
    project: str
    user: str

    @validator("project")
    def project_validator(cls, v):
        # project must contain at least 2 characters
        # (letters, numbers, and underscores)
        # and must start with a letter
        if len(v) < 2:
            raise ValueError("name must be at least 3 characters")
        if not v[0].isalpha():
            raise ValueError("name must start with a letter")
        for c in v[1:]:
            if not c.isalnum() and c != "_":
                raise ValueError("name can only contain letters, numbers, and dashes")
        return v

    @validator("user")
    def user_validator(cls, v):
        # name must contain at least 2 characters
        # (letters, numbers, and underscores)
        # and must start with a letter
        if len(v) < 2:
            raise ValueError("user name must be at least 3 characters")
        if not v[0].isalpha():
            raise ValueError("user name must start with a letter")
        for c in v[1:]:
            if not c.isalnum() and c != "_":
                raise ValueError(
                    "user name can only contain letters, numbers, and dashes"
                )
        return v


class Project(BaseModel):
    name: str

    @validator("name")
    def project_name_validator(cls, v):
        rex = re.compile(r"^[a-zA-Z][a-zA-Z0-9_]*$")
        if not rex.fullmatch(v):
            raise ValueError(
                f"project name '{v}' must be of the form ^[a-zA-Z][a-zA-Z0-9_]*$"
            )
        return v


app = FastAPI()
deta = Deta(credentials["project_key"])


@app.get("/")
def read_root():
    return {"Hello": "World"}


@app.post("/fetch")
async def fetch_project(
    project: str = Form(),
):
    project = Project(name=project)

    db = deta.Base("cmple_db")
    res = db.fetch({"project": project.name})

    if res is None:
        return {"status": "failure"}

    drive = deta.Drive("cmple_projects")
    file = drive.get(res.items[0]["user"] + "/" + project.name + ".zip")
    bytes = file.read()
    return Response(content=bytes)


@app.post("/uploadfiles/")
async def create_upload_files(
    file: UploadFile = Form(),
    user: str = Form(),
    project: str = Form(),
):
    item = Item(user=user, project=project)
    drive = deta.Drive("cmple_projects")
    drive.put(
        name=user + "/" + file.filename,
        data=await file.read(),
        content_type=file.content_type,
    )
    db = deta.Base("cmple_db")
    db.put(item.dict())
    return {"status": "success"}
