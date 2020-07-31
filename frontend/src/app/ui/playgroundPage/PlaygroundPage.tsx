import React from "react";
import dayjs from "dayjs";
import { PlanningCalendarDay } from "app/ui/planningPage/PlanningCalendarDay";
import { Box } from "@material-ui/core";
import { Workmode } from "app/services/apiClientService";
import { FormControl } from '../ux/formcontrol';
import { Select } from '../ux/select';


export const PlaygroundPage: React.FC = () => {
  const [value, setValue] = React.useState(0);
  const onChange = (event: any) => {
    setValue(event.target.value);
  };

  return (
    <div className="PlaygroundPage">
      <Box
        padding="1rem 0.5rem 2rem"
        border="1px solid grey"
      >
        <FormControl>
          <Select
            label="myLabel"
            value={value}
            onChange={onChange}
          >
            <option value={0}>NoneNoneNoneNoneNoneNoneNoneNone</option>
            <option value={1}>NoneNoneNoneNoneNoneNoneNoneNone</option>
            <option value={2}>NoneNoneNoneNoneNoneNoneNoneNonasdasdasdasdae</option>
          </Select>
        </FormControl>
      </Box>
    </div>
  );
};
