from __future__ import print_function

import sys
import glob
import os
from hashlib import md5


def findnext(list, string):
    for i in range(len(list)):
        if(list[i].strip().find(string) == 0):
            return i
    raise Exception("String not found")


def parse_fortran_template(input,output):

    # Find file checksum
    program = open(input, "rb")
    md5hash = md5(program.read()).hexdigest()
    program.close()

    # Read input file

    program = open(input, "r")
    lines = program.readlines()
    program.close()

    # Parse

    while True:

        try:
            istart = findnext(lines, "!!@FOR")
            iend = findnext(lines, "!!@END FOR")
        except:
            break

        print("  Block found from lines "+str(istart)+" to "+str(iend))

        types = lines[istart].strip().rsplit()

        for type in types[1:]:

            (long, sep, short)=type.partition(":")

            for j in range(iend-1, istart, -1):
                if "@T" in lines[j] and not "intent" in lines[j]:
                    long_new = long.replace("len=*", "len=1000")
                else:
                    long_new = long
                new=lines[j].replace("@T", long_new)
                new=new.replace("<T>", short)
                lines.insert(iend+1, new)

        for j in range(istart, iend+1):
            lines.pop(istart).strip()

    # Write output file

    output = open(output, "w")
    output.write("! MD5 of template: %s\n" % md5hash)
    output.writelines(lines)
    output.close()


for program in glob.glob(os.path.join('templates/','*_template.f90')):
    print("Processing %s" % program)
    parse_fortran_template(program, program.replace('_template.f90','.f90').replace('templates','src'))
