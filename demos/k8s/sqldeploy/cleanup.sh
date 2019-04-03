kubectl delete deployment mssql-deployment
kubectl delete PersistentVolumeClaim mssql-data
kubectl delete StorageClass azure-disk
kubectl delete service mssql-service
kubectl delete secret mssql
