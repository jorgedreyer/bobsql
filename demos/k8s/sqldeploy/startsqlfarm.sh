kubectl create secret generic mssql --from-literal=SA_PASSWORD="Sql2019isfast"
kubectl apply -f sqlfarm.yaml --record