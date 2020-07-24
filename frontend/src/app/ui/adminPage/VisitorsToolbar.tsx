import React from 'react';
import { FormControl, Select, styled, useMediaQuery } from '@material-ui/core';
import { DatePicker } from '@material-ui/pickers';
import { SelectInputProps } from '@material-ui/core/Select/SelectInput';
import dayjs from 'dayjs';

import { Flex } from '../ux/containers';
import { Button } from '../ux/buttons';
import { ICSVDataItem, IPersonMapped, IToolbar } from './types';
import { CSVButton } from './CSVButton';
import { IOfficeSpaceDto } from '../../services/apiClientService';
import { Theme } from '../ux/theme';
import { Toolbar, ToolbarItem } from './styled';


interface IVisitorsToolbar extends IToolbar {
  offices?: IOfficeSpaceDto[];

  endDate: dayjs.Dayjs;
  currentSite?: string;

  onSiteChange?: SelectInputProps['onChange'];
}

const DatePickersContainer = styled(Flex)({
  flexDirection: 'row',
  alignItems: 'center',
});

const DatePickerContainer = styled("div")({
  width: '50%',
  paddingLeft: '0.5rem',
  paddingRight: '0.5rem',
});

const visitorsTableDataToCSV = (tableData: any[]) => {
  return tableData.reduce((acc, { date, site, visitors }) => {
    const extendedVisitors: ICSVDataItem[] = visitors.map(({ name, email }: IPersonMapped) => ({
      date: date || '',
      site: site || '',
      name: name || '',
      email: email || ''
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
  const officesOptions = (offices || []).map(({ site }) => ({ value: site, label: site }));
  const csvData: ICSVDataItem[] = visitorsTableDataToCSV(tableData);
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

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
              variant={isMobile ? 'dialog' : 'inline'}
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
              variant={isMobile ? 'dialog' : 'inline'}
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
            native
            value={currentSite}
            onChange={onSiteChange}
            name="site"
            inputProps={{
              id: 'site-select',
            }}
          >
            {
              officesOptions.map(({ value, label }) => (
                <option key={value} value={value}>{label}</option>
              ))
            }
          </Select>
        </FormControl>
      </ToolbarItem>
      <ToolbarItem>
        <Button
          variant="contained"
          color="primary"
          onClick={() => onSearch()}
        >
          Search
        </Button>
      </ToolbarItem>
      <ToolbarItem>
        <CSVButton
          data={csvData}
          filename={`${currentSite}_${startDate}_${endDate}`}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
