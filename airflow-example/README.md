<!-- README: This is just to display, that I am familiar with airflow. This example was one of my homeworks in Data-Engineering course. In this project, I am responsible of the dag, that is located in dags/ directory. All the other setup (airflow and docker) was not done by me in this case. -->

## Synopsis

Airflow example - to display my ability to work with Airflow.

DISCLAIMER: the only thing I wrote/created was the DAG that is located in the dags/ directory.
The setup was taken from: https://github.com/DataSystemsGroupUT/dataeng/tree/Homework6


## Purpose

This was a homework in Data-Engineering course.
The only thing I created is the DAG, that is located in the dags/ directory.
The source of airflow and other setup is referenced in the Synopsis.
The reason for adding this homework to the showcase/ repository is to display, that I am familiar with Airflow.

## Setup

Make sure python is installed.
Also, make sure docker containers work.\
To start docker service (system with systemd) run

```sh
doas -- systemctl start docker.service
# or
sudo systemctl start docker.service
```

Download the whole repository and navigate to airflow-example:

```sh
git clone https://github.com/moledoc/showcase.git
```

or make new directory and checkout only airflow-example:

```sh
mkdir showcase
cd showcase
git init
git remote add origin https://github.com/moledoc/showcase.git
git fetch --all
git checkout origin/master -- airflow-example
```

## Build and running

In showcases/airflow-example run docker build, _once_.


```sh
doas -- docker build -t airflow-training:1.0 .
# or 
sudo docker build -t airflow-training:1.0 .
```

The start up a docker container

```sh
doas -- docker-compose -f docker-compose.yml up -d
# or
sudo docker-compose -f docker-compose.yml up -d
```

To shut the container down, run

```sh
doas -- docker-compose down
# or
sudo docker-compose down
```

To see the dags, go to http://localhost:8080.

### Demo

![Screenshot](https://github.com/moledoc/showcase/blob/master/airflow-example/demo.png)

## Author

Written by
Meelis Utt
