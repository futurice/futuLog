import React, { useEffect } from "react";
import { PageMargins, Stack, HR } from "app/ui/ux/containers";
import { H2, P } from "app/ui/ux/text";
import { LinkButton } from "app/ui/ux/buttons";

interface IWelcomePage {
  onMount: () => void;
}

export const WelcomePage: React.FC<IWelcomePage> = ({ onMount }) => {
  useEffect(onMount, []); // eslint-disable-line

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

        <LinkButton to="/" variant="contained" color="primary">
          Start
        </LinkButton>
      </Stack>
    </PageMargins>
  );
};
