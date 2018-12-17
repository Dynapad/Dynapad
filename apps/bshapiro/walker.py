import os

path="."
thumbsDir=".thumbs"

def ensureDirectory(path):
    if os.path.exists(path):
        if os.path.isdir(path):
            return "already exists"
        else:
            raise "Could Not Create Directory: %s"%path #this is a problem
    else:
        os.mkdir(path)
        return "directory created"
    


