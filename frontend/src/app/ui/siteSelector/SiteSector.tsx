import React from "react";
import { Select } from "../ux/select";
import { MenuItem } from "@material-ui/core";
import { Button } from "../ux/buttons";
import { IOfficeSpaceDto } from "app/services/apiClientService";
import { officesQueryKey } from "app/utils/reactQueryUtils";
import { useServices } from "../../services/services";
import { Stack } from "../ux/containers";
import { H4 } from "../ux/text";

interface ISiteSelector {
  handleSiteChange: (event: React.ChangeEvent<{ name?: string; value: unknown; }>) => void;
  registerUserSite: (event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => void;
  currentSite: string;
  buttonText: string;
}

export const SiteSelector: React.FC<ISiteSelector> = (props) => {
  const { queryCache } = useServices();
  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey());
  const officesOptions = (offices || []).map(({ site }) => ({ value: site, label: site }));

  return (
    <>
      <Stack spacing="1rem">
        <H4>
          Select your usual office
				</H4>
        <Select
          value={props.currentSite}
          onChange={props.handleSiteChange}
          name="site"
          inputProps={{
            id: "site-select",
          }}
        >
          {
            officesOptions.map(({ value, label }) => (
              <MenuItem
                disableRipple
                key={value}
                value={value}
              >{label}
              </MenuItem>
            ))
          }
        </Select>
        <div>
          <Button variant="contained" color="primary" disabled={props.currentSite === "" ? true : false} onClick={props.registerUserSite}>
            {props.buttonText}
          </Button>
        </div>
      </Stack>
    </>
  );
};
