import React, { useEffect, useState } from "react";
import { PageMargins, Stack, HR } from "app/ui/ux/containers";
import { H2, P } from "app/ui/ux/text";
import { useServices } from "../../services/services";
import { useMutation } from "react-query";
import { SiteSelector } from "../siteSelector/SiteSector";
import { userQueryKey, setDefaultOfficeQueryKey } from "app/utils/reactQueryUtils";
import { IUserDto } from "app/services/apiClientService";

interface IWelcomePage {
  onMount: () => void;
}

export const WelcomePage: React.FC<IWelcomePage> = ({ onMount }) => {
  const { apiClient, queryCache } = useServices();
  useEffect(onMount, []); // eslint-disable-line

  //
  // Remote data

  // These are pre-loaded in AppRoutes
  const user = queryCache.getQueryData<IUserDto>(userQueryKey())!;

  const [currentOffice, setCurrentOffice] = useState(user.defaultOffice || "");

  const [setDefaultOffice] = useMutation(
    (request: string) => apiClient.setDefaultOffice(request),
    {
      onSuccess: () => {
        queryCache.refetchQueries(setDefaultOfficeQueryKey());
        window.location.href = '/';
      },
    }
  );


  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentOffice(event.target.value as string);
  }

  const registerUserSite = (_event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    setDefaultOffice(currentOffice)
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
          currentSite={currentOffice}
          buttonText="Start" />
      </Stack>
    </PageMargins>
  );
};
