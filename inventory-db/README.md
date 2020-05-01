# Database

###Build docker image
```
$ cd inventory-db
$ docker build -t inventory-db ./ 
```

###Run DB docker container
```
$ docker run -it --net=host -p 5432:5432 -d inventory-db
```

###Run DB flyway migration
```
$ docker run --rm flyway/flyway migrate
```
OR
```
$ docker run --rm flyway/flyway -url=jdbc:postgresql://192.168.99.100:5432/inventory_db -user=inventory_user -password=
inventory_password migrate
```
OR
```
$ docker run --rm -v $PWD/inventory-db/sql:/flyway/sql -v $PWD/inventory-db/conf:/flyway/conf -v $PWD/inventory-db/jars:/flyway/jars flyway/flyway migrate
```
