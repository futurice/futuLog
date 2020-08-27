import React, { useEffect, useState } from "react";
import { PageMargins, Stack, HR } from "app/ui/ux/containers";
import { H2, H4, P } from "app/ui/ux/text";
import { Button } from "app/ui/ux/buttons";

import { Select } from "../ux/select";
import { MenuItem } from "@material-ui/core";
import { IOfficeSpaceDto, ISetShiftDto, IShiftAssignmentDto } from "app/services/apiClientService";
import { officesQueryKey, userShiftQueryKey } from "app/utils/reactQueryUtils";
import { useServices } from "../../services/services";
import { useMutation } from "react-query";

interface IWelcomePage {
  onMount: () => void;
}

export const WelcomePage: React.FC<IWelcomePage> = ({ onMount }) => {
  const { apiClient, queryCache } = useServices();
  useEffect(onMount, []); // eslint-disable-line

  //
  // Remote data

  // These are pre-loaded in AppRoutes
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey());
  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey());

  const [currentSite, setCurrentSite] = useState((userShift && userShift.site) || "");
  const officesOptions = (offices || []).map(({ site }) => ({ value: site, label: site }));

  const [registerSiteShift] = useMutation(
    (request: ISetShiftDto) => apiClient.registerSiteShift(request),
    {
      onSuccess: () => queryCache.refetchQueries(userShiftQueryKey()),
    }
  );


  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const registerUserSite = (event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    registerSiteShift({ shiftName: "default", site: currentSite })
    window.location.href = '/'

  }


  return (
    <PageMargins className="WelcomePage">
      <Stack paddingTop="2.5rem" spacing="2.5rem" maxWidth="26rem" mx="auto" textAlign="center">

        <H2>Welcome to the Futurice Entrance App!</H2>

        <HR />

        <P>
          Here you can book a spot when you need to go to the office, so we don’t exceed the maximum
          amount of people for your safety and the one of your colleagues. Please note that the
          general recommendation is still to work from home.
        </P>

        <Stack spacing="1rem">
          <H4>
            Select your usual office
        </H4>
          <Select
            value={currentSite}
            onChange={handleSiteChange}
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
        </Stack>
      </Stack>
      <Stack paddingTop="2.5rem" spacing="2.5rem" maxWidth="26rem" mx="auto" textAlign="center">
        <Button variant="contained" color="primary" disabled={currentSite === "" ? true : false} onClick={registerUserSite}>
          Start
        </Button>
      </Stack>
    </PageMargins>
  );
};
