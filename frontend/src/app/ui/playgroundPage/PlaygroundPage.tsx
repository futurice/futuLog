import React from "react";
import dayjs from "dayjs";
import { PlanningCalendarDay } from "app/ui/planningPage/PlanningCalendarDay";
import { Box } from "@material-ui/core";

export const PlaygroundPage: React.FC = () => {
  return (
    <div className="PlaygroundPage">
      <Box padding="1rem 0.5rem 2rem" border="1px solid grey">
        <PlanningCalendarDay date={dayjs().utc()} onClose={() => console.log("closed")} />
      </Box>
    </div>
  );
};
