import React from 'react';
import { styled } from '@material-ui/core';

import { Flex } from '../ux/containers';
import { P } from '../ux/text';
import { Button } from '../ux/buttons';
import { ISelectOptionString, IToolbar } from './types';
import { CSVButton } from './CSVButton';
import { IOfficeSpaceDto } from '../../services/apiClientService';


interface IVisitorsToolbar extends IToolbar {
  offices?: IOfficeSpaceDto[];

  endDate?: string;
  currentSite?: string;

  onSiteChange?: (newSite: ISelectOptionString) => void;
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
  // TODO: convert officeBookings in proper format for csv
  const csvData: any[] = tableData || [];

  return (
    <Toolbar>
      <ToolbarItem>
        <ul>
          <li>
            <P>Start date</P>
            <div>Select</div>
          </li>
          <li>
            <P>Range</P>
            <div>Select</div>
          </li>
          <li>
            <P>Person</P>
            <div>Select</div>
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
          filename={`${currentSite}_${startDate}_${endDate}`}
        />
      </ToolbarItem>
    </Toolbar>
  );
}
