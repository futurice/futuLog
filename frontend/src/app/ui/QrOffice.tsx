import React, { useState } from "react";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useParams } from "react-router-dom";
import { Box } from "@material-ui/core";
import { useQuery, useMutation } from "react-query";
import { useServices } from "app/services/services";
import {
  Workmode,
} from "app/services/apiClientService";
import {
  RenderQuery,
  combineQueries,
  officeBookingsQueryKey,
  registrationQueryKey,
} from "app/utils/reactQueryUtils";
import { LinkButton } from "app/ui/ux/buttons";
import { H2, H3, P } from "app/ui/ux/text";
import { IconCheck, IconOffice} from "app/ui/ux/icons";
import { Stack, HR } from "app/ui/ux/containers";
import { WorkmodeButton } from "app/ui/homePage/WorkmodeButtons";
import { Card } from "app/ui/ux/Card";

export const QrOffice: React.FC = () => {
  const { office } = useParams();
  const { apiClient, queryCache } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const [ userConfirmed, setUserConfirmed ] = useState(
    false
  );

  const [registerUserWorkmode] = useMutation(
    () => apiClient.setRegistrations([{ office, date, workmode: { type: Workmode.Office, confirmed:true } }]),
    {
      onSuccess: () => queryCache.refetchQueries(registrationQueryKey(date)),
    }
  );

  const [confirmUserWork] = useMutation(
    () => apiClient.confirmWorkmode(date),
    {
      onSuccess: () => queryCache.refetchQueries(registrationQueryKey(date)),
    }
  );


  const confirmUserOffice = () => {
    registerUserWorkmode()
    confirmUserWork()
    setUserConfirmed(true)
  }

  const userWorkmodeRes = useQuery(registrationQueryKey(date), () =>
    apiClient.getRegistrations(date).catch(() => null)
  );
  const officeBookingsRes = useQuery(
    officeBookingsQueryKey(office, date, date),

    () => apiClient.getOfficeBooking({ office })
  );

  if (!userConfirmed) {
    confirmUserOffice()
  }

  return(
    <Stack spacing="2.5rem" maxWidth="25rem" mx="auto" textAlign="center">

    <RenderQuery
      query={combineQueries({
        userWorkmode: userWorkmodeRes,
        officeBookings: officeBookingsRes,
      })}
      onLoading={(data, children) => children(data || ({} as any), true)}
      onError={(error, children) => children({} as any, false, error)}
    >

      {({ userWorkmode, officeBookings }, _isLoading: boolean, _error?: Error) =>
      <Stack spacing="2.5rem" maxWidth="25rem" mx="auto" textAlign="center">
        <Card spacing="2rem" textAlign="center">

          <H2>Where are you working today?</H2>
          <WorkmodeButton
            disabled={false}
            active={true}
            startIcon={<IconOffice />}
            hoverColor="jade-green-10"
            onClick={()=>{}}
          >
          Office
          </WorkmodeButton>
          <HR/>
          <Box
            component={H3}
            maxWidth="26rem"
            mx="auto"
            fontSize="1rem"
            fontWeight="bold"
            fontFamily="Futurice"
            lineHeight="1.75"
            marginBottom="0"
          >
            <IconCheck/>
            <br />

            You are checked in!
            <br />

            Thank you.
          </Box>
        </Card>
        <br />

        <Card spacing="2rem" textAlign="center">
          <Stack spacing="1.25rem" maxWidth="26rem" mx="auto">
            <H2>Where will you work in the next two weeks?</H2>
            <P>
              Plan in advance where you want to work in the next weeks.
            </P>
            <LinkButton to={RoutePaths.Planning} variant="contained" color="primary">
              Plan
            </LinkButton>
          </Stack>
        </Card>

      </Stack>

    }

  </RenderQuery>

  </Stack>

)

}
