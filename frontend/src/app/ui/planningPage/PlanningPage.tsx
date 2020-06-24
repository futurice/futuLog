import React, { useState } from "react";
import { Box, styled } from "@material-ui/core";
import { Theme } from "app/ui/ux/theme";
import { H2 } from "app/ui/ux/text";
import { PlanningCalendar } from "app/ui/planningPage/PlanningCalendar";

const Page = styled(Box)<Theme>(({ theme }) => ({
  backgroundColor: theme.colors["deep-blue-10"],
}));

const HeaderWrapper = styled("div")<Theme>(({ theme }) => ({
  position: "sticky",
  top: "64px", // NOTE: NavigationBar is 64px tall in desktop
  zIndex: 1,
  paddingTop: "2.5rem",
  color: theme.colors["deep-blue-90"],
  backgroundColor: theme.colors.white,
  boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  [theme.breakpoints.down("sm")]: {
    display: "none",
  },
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

export const PlanningPage: React.FC = () => {
  const [month, setMonth] = useState<string>();

  return (
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
            {month}
          </Box>
        </Header>
      </HeaderWrapper>

      {/* Calendar view */}
      <CalendarWrapper>
        <PlanningCalendar onChangeVisibleMonth={setMonth} />
      </CalendarWrapper>
    </Page>
  );
};
