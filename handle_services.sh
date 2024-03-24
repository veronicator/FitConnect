#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Choose the command: $0 {start|stop|status|rmc|istatus|rmi}"
    exit 1
fi

case $1 in
    start)
        sudo docker-compose up -d
        ;;
    stop)
        sudo docker stop $(sudo docker ps -a -q)
        ;;
    status)
        sudo docker ps -a
        ;;
    rmc)
        sudo docker rm $(sudo docker ps -a -q)
        ;;
    istatus)
        sudo docker images
        ;;
    rmi)
        sudo docker rmi $(sudo docker images -a -q)
        ;;
    *)
        echo "Invalid command: $1"
        echo "Choose the command: $0 {start|stop|cstatus|rmc|istatus|rmi}"
        exit 1
        ;;
esac
