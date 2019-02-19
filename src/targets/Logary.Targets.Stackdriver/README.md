# Logary Stackdriver


 - [Access Control][access-control]
 - [Roles][roles]
 - [Granting, changing and revoking access][policies]
 - [Service Accounts overview][sa-o]

You can run the following to set up a GCP service account with the right permissions;

    gcloud iam service-accounts get-iam-policy logary-ci-writer@logary-ci.iam.gserviceaccount.com
    gcloud projects add-iam-policy-binding logary-ci \
        --member serviceAccount:logary-ci-writer@logary-ci.iam.gserviceaccount.com \
        --role roles/logging.logWriter

 [access-control]: https://cloud.google.com/logging/docs/access-control
 [roles]: https://cloud.google.com/iam/docs/granting-roles-to-service-accounts
 [policies]: https://cloud.google.com/iam/docs/granting-changing-revoking-access
 [sa-o]: https://cloud.google.com/iam/docs/understanding-service-accounts
