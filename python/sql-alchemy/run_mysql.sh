docker pull mysql
docker run -it --name example -e MYSQL_ROOT_PASSWORD=root -d mysql:latest

# please create `example` db manually

# docker exec -it example bash -p
# mysql -u root -p -h 127.0.0.1
# mysql> create database example;
