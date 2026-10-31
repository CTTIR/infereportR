#!/usr/bin/env python3
"""Build/install reporting source with the proposed namespace in an isolated copy."""
from pathlib import Path
import argparse, json, os, shutil, subprocess, time
parser=argparse.ArgumentParser()
parser.add_argument('source'); parser.add_argument('output'); parser.add_argument('calc_library')
parser.add_argument('--integration-library');parser.add_argument('--full',action='store_true')
a=parser.parse_args();src=Path(a.source).resolve();out=Path(a.output).resolve()
out.mkdir(parents=True,exist_ok=False);copy=out/'source'/'infereportR'
shutil.copytree(src,copy,ignore=shutil.ignore_patterns('.git'))
shutil.copyfile(copy/'tools/reporting/NAMESPACE.proposed',copy/'NAMESPACE')
description=copy/'DESCRIPTION'
description.write_text(description.read_text().replace('Imports: infercalcR, stats, utils','Imports: infercalcR, utils').replace('Suggests: ggplot2, DescrTab2','Suggests: ggplot2 (>= 3.4.0), DescrTab2'))
(copy/'.Rbuildignore').write_text('^tools$\n^\\.gitignore$\n^\\.Rbuildignore$\n')
lib=out/'library';lib.mkdir();empty=out/'empty-site';empty.mkdir()
# Only the provider is copied into the minimal library. No site packages leak in.
shutil.copytree(Path(a.calc_library)/'infercalcR',lib/'infercalcR')
env=os.environ.copy();env.update(R_LIBS_USER=str(lib),R_LIBS_SITE=str(empty),R_LIBS='',_R_CHECK_FORCE_SUGGESTS_='false',INFER_SUITE_PLOT_DIR=str(out/'plots'),INFER_SUITE_REPORT_DIR=str(out/'native-render'))
if a.integration_library:
    env['R_LIBS']=a.integration_library
if a.full:env['INFER_SUITE_FULL']='true'
else:env['INFER_SUITE_FULL']='false'
records=[]
commands=[('build',['R','CMD','build',str(copy)]),('install',['R','CMD','INSTALL','--install-tests','--library='+str(lib),str(out/'infereportR_0.0.0.9000.tar.gz')]),('check',['R','CMD','check','--no-manual',str(out/'infereportR_0.0.0.9000.tar.gz')])]
for name,cmd in commands:
    started=time.time()
    with (out/(name+'.log')).open('w') as log:result=subprocess.run(cmd,cwd=out,env=env,stdout=log,stderr=subprocess.STDOUT)
    record=dict(check=name,command=cmd,exit_code=result.returncode,elapsed=time.time()-started)
    records.append(record);(out/'commands.json').write_text(json.dumps(records,indent=2));print(record,flush=True)
    if result.returncode:break
