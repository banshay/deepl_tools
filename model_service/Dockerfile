FROM nvidia/cuda:12.3.1-base-ubuntu20.04

RUN apt-get update && \
    apt-get install -y python3 python3-pip python3-dev gcc && \
 rm -rf /var/lib/apt/lists/*

WORKDIR /scratch

COPY . /scratch

RUN pip install --no-cache-dir -r requirements.txt

EXPOSE 5000

CMD ["python3", "app.py"]
