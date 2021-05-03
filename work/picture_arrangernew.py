import os
import shutil

sdir = r'C:\Users\info\Documents\KALZIUM vorübergehend'

dest = r'C:\Users\info\Documents\KALZIUM vorübergehend\alle'

for root, subdirs, files in os.walk(sdir):   
    for file in files:
        path = os.path.join(root,file)
        strpath = str(file)
        cond = True
        for char in strpath:
            if char == '_' or char == 'j':
                print(strpath)
                cond = False
                break

        if cond:
            shutil.move(path,dest)
            print("Success! Moved", strpath, "to destination folder.")
