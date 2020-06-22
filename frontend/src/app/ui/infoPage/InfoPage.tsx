import React from "react";
import { PageMargins, Stack } from "app/ui/ux/containers";
import { H2, H3, P, H4 } from "app/ui/ux/text";
import { Box } from "@material-ui/core";

export const InfoPage: React.FC = () => (
  <PageMargins className="InfoPage">
    <Stack spacing="2.5rem" maxWidth="38rem" mx="auto">
      <Box textAlign="center">
        <H2>Information</H2>
      </Box>

      <Stack spacing="1.5rem">
        <H3>What is the Check-in App about?</H3>

        <P>
          Now that the number of daily new infections with COVID-19 is slowly decreasing and the
          restrictions of public life and the world of work are being eased, a visit to the office
          seems possible again. But because the virus doesn't just disappear, we can't just go back
          to normal in an instant. Instead, we need to coordinate office presence carefully.
        </P>
        <P>
          The main purpose of this Check-in App is to help building a safe environment for everybody
          at Futurice. This happens via two basic functionalities:
        </P>
        <P>1.Limiting the number of people in the office to enable social distancing.</P>
        <P>
          2. Tracking who went to the office on which day to inform the according people in case of
          an incident. All companies are obliged to keep such a record.
        </P>
        <H4>Important to know</H4>
        <P>
          The general recommendation is still to work from home. Please consider this first when
          thinking about if you really have to go to the office.
        </P>
      </Stack>

      <Stack spacing="1.5rem">
        <H3>How to use the Check-In App? </H3>

        <H4>Getting started</H4>
        <P>
          Check-In App contains a central function that is displayed directly when you call up the
          page: You will be asked where you are working from today. The answer to this question is
          "Home Office" by default. If you plan to go to the office today, you can select the
          appropriate option ("Office") and then confirm that you are actually going. Each Futurice
          Office has a limit on how many people can be in the office at the same time to enable
          social distancing. Once this limit is exceeded, it is no longer possible for anyone to
          make further bookings for that day. In every Futurice office you will find additional
          posters with more detailed information about the space setups and what rules to follow.
        </P>
        <H4>Planning future days at the office</H4>
        <P>
          You prefer to plan your days in advance? You have an important workshop in two weeks for
          which you absolutely have to be in the office? No problem, we got you covered. Using the
          planning function, which you can see on the start page as the second highest, you can call
          up a calendar and secure a place in the office for certain future days.
        </P>
        <P>
          Planning over several days is also possible, but only for the Home Office / at Client
          Premise / on Leave options. As long as working in the office is a limited resource,
          everyone should get a fair chance every day.
        </P>
        <H4>Who else is in the office that day?</H4>
        <P>
          To many of you the decision of going to the office might depend on who else is going. We
          are working on a way to integrate this feature into the system in the next weeks.
        </P>
      </Stack>

      <Stack spacing="1.5rem">
        <H3>What to do in case the office is fully booked but I really need to go?</H3>

        <P>
          If you really need to go to the office, but it’s fully booked and you can’t get a spot
          anymore, try the following:
        </P>

        <Stack component="ul" spacing="1.5rem">
          <li>Getting in contact with the HC team of your local office</li>
          <li>Post you need on Slack and see if somebody might want to change days with you</li>
          <li>Rethink if you really, really have to go ;)</li>
        </Stack>

        <P>
          Please avoid simply going to the office without it being tracked somewhere. If you
          actually go without telling anybody, you might potentially risk the health of others
          around you and your own.
        </P>
      </Stack>

      <Stack spacing="1.5rem">
        <H3>I just need to print something, do I need to check-in anyways?</H3>

        <P>
          The potential risk for infecting yourself or others also exists if you only enter the
          office for printing a sheet of paper. That’s why we would kindly ask you to book a spot at
          the office if you want to go, or try printing somewhere else maybe?
        </P>
      </Stack>

      <Stack spacing="1.5rem">
        <H3>Oops – I went to the office but didn’t confirm!</H3>

        <P>
          This can happen. In this case, we would advise to talk to your local HC team which will
          track your presence in the office manually for that day.{" "}
        </P>
      </Stack>
    </Stack>
  </PageMargins>
);
