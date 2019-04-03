PROJECT_ID="crucial-baton-203418"
gcloud iam service-accounts create "logary-ci" --display-name "logary-ci"
gcloud iam service-accounts keys create "logary-ci.json" --iam-account "logary-ci@${PROJECT_ID}.iam.gserviceaccount.com"
gcloud pubsub topics create logs
gcloud iam roles create logary.ci --project "${PROJECT_ID}" --file logary-ci-role.yaml
