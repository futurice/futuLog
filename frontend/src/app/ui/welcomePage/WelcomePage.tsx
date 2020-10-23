import React, { useEffect, useState } from "react";
import { PageMargins, Stack, HR } from "app/ui/ux/containers";
import { H2, P } from "app/ui/ux/text";
import { ISetShiftDto, IShiftAssignmentDto } from "app/services/apiClientService";
import { userShiftQueryKey } from "app/utils/reactQueryUtils";
import { useServices } from "../../services/services";
import { useMutation } from "react-query";
import { SiteSelector } from "../siteSelector/SiteSector";

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

  const [currentSite, setCurrentSite] = useState((userShift && userShift.site) || "");

  const [registerSiteShift] = useMutation(
    (request: ISetShiftDto) => apiClient.registerSiteShift(request),
    {
      onSuccess: () => {
        queryCache.refetchQueries(userShiftQueryKey());
        window.location.href = '/';
      },
    }
  );


  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const registerUserSite = (event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    registerSiteShift({ shiftName: "default", site: currentSite })
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

        <SiteSelector
          handleSiteChange={handleSiteChange}
          registerUserSite={registerUserSite}
          currentSite={currentSite}
          buttonText="Start" />
      </Stack>
    </PageMargins>
  );
};
