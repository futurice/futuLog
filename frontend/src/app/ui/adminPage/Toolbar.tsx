import { CSVLink } from 'react-csv';
import React from 'react';
import { styled } from '@material-ui/core';

import { Flex } from '../ux/containers';
import { P } from '../ux/text';
import { Button } from '../ux/buttons';


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

export interface ITableToolbar {
  site?: string,
  startDate: string,
  endDate?: string,
  range?: string,
  changeDate?: () => {},
  user?: any,
  users?: string[],
  offices?: string[]
}


export function TableToolbar ({
  site,
  startDate,
  endDate,
  range,
  user,
  users,
  offices,
  changeDate
}: ITableToolbar) {
  return (
    <Toolbar>
      <ToolbarItem>
        <div>
          <P>Where?</P>
          <P>From ......... to .......... </P>
        </div>
        <div>
          <P>Office</P>
          <Button
            variant="contained"
            color="primary"
          >
            set office
          </Button>
        </div>
        <Button
          variant="contained"
          color="primary"
        >
          Search
        </Button>
      </ToolbarItem>
      <ToolbarItem>
        {/* TODO: convert officeBookings in proper format for csv*/}
        <CSVLink
          style={{ textDecoration: 'none' }}
          data={[]}
          filename={`${site}_${startDate}_${endDate}.csv`}
          rel="noreferrer noopener"
          target="_blank"
        >
          <Button
            variant="outlined"
            color="primary"
          >
            Export list
          </Button>
        </CSVLink>

      </ToolbarItem>
    </Toolbar>
  );
}
