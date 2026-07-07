.PHONY: reproduce reproduce-python reproduce-r verify install docker-build docker-run clean

# Reproduce todo el análisis end-to-end.
reproduce:
	$(MAKE) reproduce-python
	$(MAKE) reproduce-r
	$(MAKE) verify

reproduce-python:
	python -m pip install -r environment/requirements.txt
	cd code && python pipeline.py

reproduce-r:
	Rscript -e "if(!requireNamespace('renv',quietly=TRUE)) install.packages('renv'); renv::restore(lockfile='environment/renv.lock')"
	cd code && Rscript pipeline.R

# Verifica que los archivos en el árbol coincidan con MANIFEST.json.
verify:
	python -c "import json,hashlib,sys;from pathlib import Path;m=json.loads(Path('MANIFEST.json').read_text());bad=0\nfor rel,h in (m.get('checksums') or {}).items():\n  p=Path(rel)\n  if not p.exists(): continue\n  a=hashlib.sha256(p.read_bytes()).hexdigest()\n  if a!=h: print('drift:',rel); bad+=1\nsys.exit(1 if bad else 0)"

# Construye el contenedor reproducible y ejecuta el pipeline dentro.
docker-build:
	docker build -t ressearch-package:latest .

docker-run: docker-build
	docker run --rm -v $(PWD):/work -w /work ressearch-package:latest make reproduce

clean:
	rm -rf _local_outputs/ .venv/ __pycache__/ .Rhistory
