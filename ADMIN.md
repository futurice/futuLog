# Things to do as an admin

All these commands will fail if your email is not in the [admins.yaml](./admins.yaml) that is running in production. So if you add a new admin, you need to deploy!

## Setting the shift for a new user

If a new user has to be added to the system, you need to create a csv file named `futulog.csv` with the following content:

```csv
Name,Email,Office,Shift name
Test User,test-user@futurice.com,Berlin,default
```

Possible values for `Office` are: `Berlin`, `Munich`, `Stockholm` and `Stuttgart`. The shift name is always `default`.

You can add more than one user in one go by adding more lines to the CSV.

To add all of the users run:

```bash
export FUM_COOKIE=<cookie content from the browser> # Starts with `uid%3D`
curl -F file=@futulog.csv --cookie "auth_pubtkt=$FUM_COOKIE" https://futulog.play.futurice.com/api/admin/shift/csv/add
```

## Exporting a CSV with the people that were in the office

Run:

```bash
export START_DATE=2020-06-22 # Use own date here
export END_DATE=2020-07-09   # Use own date here
export OFFICE=Munich # See allowed office names above
export FUM_COOKIE=<cookie content from the browser>
curl -XGET -o workmodes.csv --cookie "auth_pubtkt=$FUM_COOKIE" "https://futulog.play.futurice.com/api/admin/workmode/csv/$OFFICE?startDate=$START_DATE&endDate=$END_DATE"
```

Now have downloaded a `workmodes.csv` file that contains all the data
