import re
import subprocess


flags = re.compile('CX*FLAGS=(.*)')
libs = re.compile('LIBS=(.*)')


def GetLibFlags(liblist):
    args = ['pkg-config', '--cflags'] + liblist
    kwargs = {'universal_newlines': True}
    return subprocess.check_output(args, **kwargs).strip()


def ReadFile(fname):
    with open(fname) as f:
        return f.read()


def ParseMakefile():
    makefile = ReadFile('makefile')
    liblist = libs.findall(makefile)[0].split()
    result = flags.findall(makefile) + [GetLibFlags(liblist)]
    return ' '.join(result).split()


def FlagsForFile(filename, **kwargs):
    return {
        'do_cache': True,
        'flags': ParseMakefile()
    }
