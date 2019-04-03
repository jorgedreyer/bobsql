az aks create --name bwsqlaks \
--resource-group bwsqldemos \
--generate-ssh-keys \
--node-vm-size Standard_L4s \
--node-count 3 \
--kubernetes-version 1.12.5
