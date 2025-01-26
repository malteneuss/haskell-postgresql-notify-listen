default:
    @just --list

dbup:
    docker-compose -f ./docker-compose.yml up -d
dbdown:
    docker-compose -f ./docker-compose.yml down -v
