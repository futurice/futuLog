import React from 'react';
import { FormControl, InputLabel, Select, styled } from '@material-ui/core';

import { Flex } from '../ux/containers';
import { P } from '../ux/text';
import { Button } from '../ux/buttons';
import { ICSVDataItem, IPersonMapped, IToolbar } from './types';
import { CSVButton } from './CSVButton';
import { IUsersDto } from '../../services/apiClientService';
import { SelectContainer } from './VisitorsToolbar';
import { SelectInputProps } from '@material-ui/core/Select/SelectInput';


interface ITrackingToolbar extends IToolbar {
  users?: IUsersDto[];

  currentUser: string;
  range: number;

  onUserChange?: SelectInputProps['onChange']
  onRangeChange?: SelectInputProps['onChange']
}

const Toolbar = styled(Flex)({
  marginBottom: '30px'
});

const ToolbarItem = styled(Flex)({
  alignItems: 'flex-end',

  '&:last-child': {
    marginLeft: 'auto',
  },

  '& > *:nth-child(n+2)': {
    marginLeft: '45px'
  },
  '& > *:last-child': {
    marginLeft: '30px'
  }
});

const trackingTableDataToCSV = (tableData: any[]) => {
  return tableData.reduce((acc, { date, site, visitors }) => {
    const extendedVisitors: ICSVDataItem[] = visitors.map(({ name, email }: IPersonMapped) => ({
      date: date || '',
      site: site || '',
      name: name || '',
      email: email || ''
    }));

    return [ ...acc, ...extendedVisitors];
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
  const usersOptions = (users || []).map(({ name }) => ({ value: name, label: name }));
  // TODO @egor make constant range
  const rangeOptions = [{ value: 14, label: '14 days' }, { value: 1, label: '1 day' }];
  const csvData: ICSVDataItem[] = trackingTableDataToCSV(tableData);

  return (
    <Toolbar>
      <ToolbarItem>
        <SelectContainer>
          <P>Start date</P>
          <div>Calendar</div>
        </SelectContainer>
        <SelectContainer>
          <FormControl>
            <InputLabel htmlFor="range-select">Range</InputLabel>
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
                rangeOptions.map(({ value, label }) => (
                  <option value={value}>{label}</option>
                ))
              }
            </Select>
          </FormControl>
        </SelectContainer>
        <SelectContainer>
          <FormControl>
            <InputLabel htmlFor="person-select">Person</InputLabel>
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
                  <option value={value}>{label}</option>
                ))
              }
            </Select>
          </FormControl>
        </SelectContainer>
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
          filename={`${currentUser}_${startDate}_${range}_day(-s)`}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
