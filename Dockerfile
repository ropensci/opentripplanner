FROM rocker/geospatial:latest

RUN install2.r --error --deps TRUE opentripplanner

RUN apt-get update && apt-get install openjdk-8-jdk -y

RUN update-alternatives --set java /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java
