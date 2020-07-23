import React, { ChangeEvent } from 'react';
import { FormControl, InputLabel, Select, styled } from '@material-ui/core';

import { Flex } from '../ux/containers';
import { P } from '../ux/text';
import { Button } from '../ux/buttons';
import { ICSVDataItem, IPersonMapped, ISelectOptionString, IToolbar } from './types';
import { CSVButton } from './CSVButton';
import { IOfficeSpaceDto } from '../../services/apiClientService';
import { SelectInputProps } from '@material-ui/core/Select/SelectInput';


interface IVisitorsToolbar extends IToolbar {
  offices?: IOfficeSpaceDto[];

  endDate?: string;
  currentSite?: string;

  onSiteChange?: SelectInputProps['onChange'];
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

export const SelectContainer = styled(Flex)({
  display: 'flex',
  flexDirection: 'column',
});

const visitorsTableDataToCSV = (tableData: any[]) => {
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

export function VisitorsToolbar ({
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

  return (
    <Toolbar>
      <ToolbarItem>
        <SelectContainer>
          <P>From</P>
          <div>Calendar</div>
        </SelectContainer>
        <SelectContainer>
          <P>to</P>
          <div>Calendar</div>
        </SelectContainer>
        <SelectContainer>
          <FormControl>
            <InputLabel htmlFor="site-select">Office</InputLabel>
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
          filename={`${currentSite}_${startDate}_${endDate}`}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
