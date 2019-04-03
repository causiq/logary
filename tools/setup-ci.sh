PROJECT_ID="crucial-baton-203418"
EMAIL="logary-ci@${PROJECT_ID}.iam.gserviceaccount.com"
gcloud iam service-accounts create "logary-ci" --display-name "logary-ci"
gcloud iam service-accounts keys create "logary-ci.json" --iam-account "$EMAIL"
gcloud pubsub topics create logs
gcloud iam roles create logary.ci --project "${PROJECT_ID}" --file logary-ci-role.yaml
bq --format=json show logs | jq ". * {access: [.access[], {\"role\":\"WRITER\",\"userByEmail\":\"$EMAIL\"}] }" >logary-ci-bq.json
bq update --source=logary-ci-bq.json logs
rm logary-ci-bq.json
