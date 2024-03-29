import React from "react";
import { MenuItem, useMediaQuery } from "@material-ui/core";
import { SelectInputProps } from "@material-ui/core/Select/SelectInput";
import { DatePicker } from "@material-ui/pickers";

import { Button } from "../ux/buttons";
import { ICSVDataItem, IUserDtoMapped, IToolbar, ITableDataDto } from "./types";
import { CSVButton } from "./CSVButton";
import { IUserDto } from "../../services/apiClientService";
import { Toolbar, ToolbarItem } from "./styled";
import { Theme } from "../ux/theme";
import { FormControl } from "../ux/formcontrol";
import { Select } from "../ux/select";
import { Searchbox } from "../ux/searchbox";
import { TextField } from "../ux/inputs";

export const DAYS_RANGE_OPTIONS = [
  { value: 1, label: "1 day" },
  { value: 14, label: "14 days" },
  { value: 30, label: "30 days" },
];

interface ITrackingToolbar extends IToolbar {
  users?: IUserDto[];
  currentUser: string;
  range: number;
  onUserChange?: (event: React.ChangeEvent<{}>, value: any | null) => void;
  onRangeChange?: SelectInputProps["onChange"];
}

const trackingTableDataToCSV = (tableData: ITableDataDto[]) => {
  return tableData.reduce((acc: ICSVDataItem[], { date, office, visitors }: ITableDataDto) => {
    const extendedVisitors: ICSVDataItem[] = visitors.map(({ name, email }: IUserDtoMapped) => ({
      date: date || "",
      office: office || "",
      name: name || "",
      email: email || "",
    }));

    return [...acc, ...extendedVisitors];
  }, []);
};

export function TrackingToolbar({
  users,
  startDate,
  range,
  currentUser,
  tableData,

  onUserChange,
  onRangeChange,
  onDateChange,
  onSearch,
}: ITrackingToolbar) {
  const usersOptions = (users || []).map(({ name, email }) => ({
    value: email,
    label: name,
  }));
  const csvData: ICSVDataItem[] = trackingTableDataToCSV(tableData);
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const startDateStr = startDate.format("YYYY-MM-DD");

  return (
    <Toolbar>
      <ToolbarItem>
        <b>Start date</b>
        <DatePicker
          value={startDate}
          format="D MMM YYYY"
          variant={isMobile ? "dialog" : "inline"}
          onChange={(date: any) => onDateChange(date, date)}
        />
      </ToolbarItem>
      <ToolbarItem>
        <b>Range</b>
        <FormControl>
          <Select
            value={range}
            onChange={onRangeChange}
            name="range"
            inputProps={{
              id: "range-select",
            }}
          >
            {
              DAYS_RANGE_OPTIONS.map(({ value, label }) => (
                <MenuItem
                  disableRipple
                  key={value}
                  value={value}
                >{label}</MenuItem>
              ))
            }
          </Select>
        </FormControl>
      </ToolbarItem>
      <ToolbarItem>
        <b>Person</b>
        <FormControl>
          <Searchbox
            id="Person-Searchbox"
            options={usersOptions}
            getOptionLabel={(option) => option.label}
            style={{ width: 200 }}
            onChange={onUserChange}
            renderInput={(params) => <TextField {...params} />}
          />
        </FormControl>
      </ToolbarItem>
      <ToolbarItem>
        <Button
          variant="contained"
          color="primary"
          onClick={onSearch}
          disabled={!startDate || !range || !currentUser}
          >
          Search
        </Button>
      </ToolbarItem>
      <ToolbarItem>
        <CSVButton
          data={csvData}
          filename={`${currentUser}_${startDateStr}_last_${range}_day(-s)`}
          disabled={!tableData || tableData.length === 0}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
