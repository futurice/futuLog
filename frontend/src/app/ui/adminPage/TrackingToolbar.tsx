import React from 'react';
import { styled } from '@material-ui/core';

import { Flex } from '../ux/containers';
import { P } from '../ux/text';
import { Button } from '../ux/buttons';
import { ISelectOptionNumber, ISelectOptionString, IToolbar } from './types';
import { CSVButton } from './CSVButton';
import { IUsersDto } from '../../services/apiClientService';


interface ITrackingToolbar extends IToolbar {
  users?: IUsersDto[];

  currentUser: string;
  range: number;

  onUserChange: (newUser: ISelectOptionString) => void;
  onRangeChange: (newRange: ISelectOptionNumber, type: string) => void;
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
  // TODO: convert officeBookings in proper format for csv
  const csvData: any[] = tableData || [];

  return (
    <Toolbar>
      <ToolbarItem>
        <ul>
          <li>
            <P>Where?</P>
            <div>
              <P>From </P>
              <div>Select</div>
              <P>to </P>
              <div>Select</div>
            </div>
          </li>
          <li>
            <P>Office</P>
            <Button
              variant="contained"
              color="primary"
            >
              set office
            </Button>
          </li>
        </ul>
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
