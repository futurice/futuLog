import React from 'react';
import { FormControl, Select, useMediaQuery } from '@material-ui/core';
import { SelectInputProps } from '@material-ui/core/Select/SelectInput';
import { DatePicker } from '@material-ui/pickers';

import { Button } from '../ux/buttons';
import { ICSVDataItem, IUserDtoMapped, IToolbar, ICapacityDtoMapped } from './types';
import { CSVButton } from './CSVButton';
import { IUserDto } from '../../services/apiClientService';
import { Toolbar, ToolbarItem } from './styled';
import { Theme } from '../ux/theme';


const DAYS_RANGE_OPTIONS = [
  { value: 1, label: '1 day' },
  { value: 14, label: '14 days' },
  { value: 30, label: '30 days' }
];

interface ITrackingToolbar extends IToolbar {
  users?: IUserDto[];

  currentUser: string;
  range: number;

  onUserChange?: SelectInputProps['onChange']
  onRangeChange?: SelectInputProps['onChange']
}

const trackingTableDataToCSV = (tableData: ICapacityDtoMapped[]) => {
  return tableData.reduce((acc: ICSVDataItem[], { date, site, visitors }: ICapacityDtoMapped) => {
    const extendedVisitors: ICSVDataItem[] = visitors.map(({ name, email }: IUserDtoMapped) => ({
      date: date || '',
      site: site || '',
      name: name || '',
      email: email || ''
    }));

    return [...acc, ...extendedVisitors];
  }, []);
}

export function TrackingToolbar({
  users,
  startDate,
  range,
  currentUser,
  tableData,

  onUserChange,
  onRangeChange,
  onDateChange,
  onSearch
}: ITrackingToolbar) {
  const usersOptions = (users || []).map(
    ({ first_name, last_name, email }) => ({ value: email, label: `${first_name} ${last_name}` })
  );
  const csvData: ICSVDataItem[] = trackingTableDataToCSV(tableData);
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  return (
    <Toolbar>
      <ToolbarItem>
        <b>Start date</b>
        <DatePicker
          value={startDate}
          format="D MMM YYYY"
          variant={isMobile ? "dialog" : "inline"}
          onChange={(date: any) =>
            onDateChange(date, date)
          }
        />
      </ToolbarItem>
      <ToolbarItem>
        <b>Range</b>
        <FormControl>
          <Select
            native
            value={range}
            onChange={onRangeChange}
            name="range"
            inputProps={{
              id: 'range-select',
            }}
          >
            {
              DAYS_RANGE_OPTIONS.map(({ value, label }) => (
                <option
                  key={value}
                  value={value}
                >{label}</option>
              ))
            }
          </Select>
        </FormControl>
      </ToolbarItem>
      <ToolbarItem>
        <b>Person</b>
        <FormControl>
          <Select
            native
            value={currentUser}
            onChange={onUserChange}
            name="person"
            inputProps={{
              id: 'person-select',
            }}
          >
            {
              usersOptions.map(({ value, label }) => (
                <option
                  key={value}
                  value={value}
                >{label}</option>
              ))
            }
          </Select>
        </FormControl>
      </ToolbarItem>
      <ToolbarItem>
        <Button
          variant="contained"
          color="primary"
          onClick={() => onSearch('user')}
        >
          Search
        </Button>
      </ToolbarItem>
      <ToolbarItem>
        <CSVButton
          data={csvData}
          filename={`${currentUser}_${startDate}_${range}_day(-s)`}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
