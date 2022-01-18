# Etools_IPs_PDs_IATI
R Script to download IPs/PDs information from eTools
## Before you run it
And `auth.R` file should be created inside `secret` directory with exact below variables: 
`BASE_URL`:   storing the base_url of the eTools API
`USER`: user email (official email address)
`PASS`: the password generated by eTools APIs, note that this is not the password of email address

Once you are done `auth.R` file, you can run `download_data.R` file, you can change some filters 
on the top of the `download_data.R` file e.g. `country_name`, `status_filter`. 