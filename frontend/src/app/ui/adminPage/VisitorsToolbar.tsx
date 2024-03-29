import React from "react";
import { MenuItem, styled, useMediaQuery } from "@material-ui/core";
import { DatePicker } from "@material-ui/pickers";
import { SelectInputProps } from "@material-ui/core/Select/SelectInput";
import dayjs from "dayjs";

import { Flex } from "../ux/containers";
import { Button } from "../ux/buttons";
import { ICSVDataItem, IUserDtoMapped, IToolbar, ITableDataDto } from "./types";
import { CSVButton } from "./CSVButton";
import { IOfficeDto } from "../../services/apiClientService";
import { Theme } from "../ux/theme";
import { Toolbar, ToolbarItem } from "./styled";
import { FormControl } from "../ux/formcontrol";
import { Select } from "../ux/select";


interface IVisitorsToolbar extends IToolbar {
  offices?: IOfficeDto[];

  endDate: dayjs.Dayjs;
  currentSite?: string;

  onSiteChange?: SelectInputProps["onChange"];
}

const DatePickersContainer = styled(Flex)({
  flexDirection: "row",
  alignItems: "center",
});

const DatePickerContainer = styled("div")({
  width: "50%",
  paddingLeft: "0.5rem",
  paddingRight: "0.5rem",
});

const visitorsTableDataToCSV = (tableData: ITableDataDto[]) => {
  return tableData.reduce((acc: ICSVDataItem[], { date, office, visitors }: ITableDataDto) => {
    const extendedVisitors: ICSVDataItem[] = visitors.map(({ name, email }: IUserDtoMapped) => ({
      date: date || "",
      office: office || "",
      name: name || "",
      email: email || ""
    }));

    return [...acc, ...extendedVisitors];
  }, []);
}

export function VisitorsToolbar({
  offices,
  startDate,
  endDate,
  currentSite,
  tableData,

  onSiteChange,
  onDateChange,
  onSearch
}: IVisitorsToolbar) {
  const officesOptions = (offices || []).map(({ name }) => ({ value: name, label: name }));
  const csvData: ICSVDataItem[] = visitorsTableDataToCSV(tableData);
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  return (
    <Toolbar>
      <ToolbarItem>
        <b>When?</b>
        <DatePickersContainer>
          <span>From</span>
          <DatePickerContainer>
            <DatePicker
              value={startDate}
              format="D MMM YYYY"
              variant={isMobile ? "dialog" : "inline"}
              onChange={(date: any) =>
                onDateChange(date, endDate.isBefore(date) ? date : endDate)
              }
            />
          </DatePickerContainer>
          <span>to</span>
          <DatePickerContainer>
            <DatePicker
              value={endDate}
              minDate={startDate}
              variant={isMobile ? "dialog" : "inline"}
              format="D MMM YYYY"
              onChange={(date: any) => onDateChange(startDate, date)}
            />
          </DatePickerContainer>
        </DatePickersContainer>
      </ToolbarItem>
      <ToolbarItem>
        <b>Where?</b>
        <FormControl>
          <Select
            value={currentSite}
            onChange={onSiteChange}
            name="site"
            inputProps={{
              id: "site-select",
            }}
          >
            {
              officesOptions.map(({ value, label }) => (
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
        <Button
          variant="contained"
          color="primary"
          onClick={onSearch}
          disabled={!startDate || !endDate || !currentSite}
        >
          Search
        </Button>
      </ToolbarItem>
      <ToolbarItem>
        <CSVButton
          data={csvData}
          filename={`${currentSite}_${startDateStr}_${endDateStr}`}
          disabled={!tableData || tableData.length === 0}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
