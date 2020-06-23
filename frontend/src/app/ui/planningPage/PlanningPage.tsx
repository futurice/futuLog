import React from "react";
import { Box, styled } from "@material-ui/core";
import { Theme } from "app/ui/ux/theme";
import { H2 } from "app/ui/ux/text";
import { PlanningCalendar } from "app/ui/planningPage/PlanningCalendar";

const Page = styled(Box)<Theme>(({ theme }) => ({
  backgroundColor: theme.colors["deep-blue-10"],
}));

const HeaderWrapper = styled(Box)<Theme>(({ theme }) => ({
  paddingTop: "2.5rem",
  color: theme.colors["deep-blue-90"],
  backgroundColor: theme.colors.white,
  boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
}));

const Header = styled(Box)<Theme>(({ theme }) => ({
  maxWidth: "72rem",
  margin: "0 auto",
  textAlign: "center",
}));

const CalendarWrapper = styled(Box)({
  maxWidth: "72rem",
  margin: "0 auto",
});

export const PlanningPage: React.FC = () => (
  <Page className="PlanningPage">
    {/* Heading element */}
    <HeaderWrapper>
      <Header>
        <H2>Where will you work in the next two weeks?</H2>
        <Box
          textAlign="left"
          paddingBottom="0.5rem"
          fontSize="1.25rem"
          fontWeight="bold"
          lineHeight="1"
        >
          June ***
        </Box>
      </Header>
    </HeaderWrapper>

    {/* Calendar view */}
    <CalendarWrapper>
      <PlanningCalendar />
    </CalendarWrapper>
  </Page>
);
