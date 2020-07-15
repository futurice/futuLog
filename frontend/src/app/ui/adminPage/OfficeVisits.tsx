import React, { useState } from 'react';
import { useQuery } from 'react-query';
import {
  Table,
  TableBody,
  TableCell,
  TableRow,
  TableHead,
  styled
} from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { colors } from '../ux/theme';
import { combineQueries, officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { Button } from '../ux/buttons';
import { Flex } from '../ux/containers';
import { P } from '../ux/text';


const childTableHead: ICollapsibleTableHead[] = [
  {
    title: 'nr'
  },
  {
    title: 'Name'
  },
  {
    title: 'Email'
  }
];

const parentTableHead: ICollapsibleTableHead[] = [
  {
    title: '',
    width: '5%'
  },
  {
    title: 'Date',
    width: '18%'
  },
  {
    title: 'Office',
    width: '15%'
  },
  {
    title: 'Capacity utilisation',
    width: '12%'
  },
  {
    title: '',
    width: '50%'
  }
];

export function createData(
  date: string,
  office: string,
  capacityUtilisation: number
) {
  return {
    date,
    office,
    capacityUtilisation,
    visitors: [
      { name: 'Anonymous123', email: 'anonymous123@anonymous.anonymous' },
      { name: 'Anonymous456', email: 'anonymous456@anonymous.anonymous' },
    ],
  };
}

const rows = [
  createData('2020-07-12', 'Berlin', 6),
  createData('2020-07-13', 'Berlin', 6),
  createData('2020-07-14', 'Berlin', 6),
];

const useChildTableStyles = makeStyles({
  root: {
    boxShadow: 'none',
    backgroundColor: 'transparent'
  }
})

const useChildCellStyles = makeStyles({
  root: {
    border: 'none',
  }
});

const useTableHeadCellStyles = makeStyles({
  root: {
    borderTop: `1px solid ${colors['deep-blue-20']}`,
    borderBottom: `1px solid ${colors['deep-blue-20']}`
  }
});

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


const Visitors = ({ row, head }: { row: ReturnType<typeof createData>, head: ICollapsibleTableHead[] }) => {
  const tableClasses = useChildTableStyles();
  const cellClasses = useChildCellStyles();
  const headCellClasses = useTableHeadCellStyles();

  return (
    <Table
      className={tableClasses.root}
      size="small"
      aria-label="visitors"
    >
      <TableHead>
        <TableRow>
          {
            head.map(({ align = 'left', title }: ICollapsibleTableHead) =>
              <TableCell
                className={headCellClasses.root}
                align={align}
              >{title}</TableCell>
            )}
        </TableRow>
      </TableHead>
      <TableBody>
        {row.visitors.map((visitorRow, i) => (
          <TableRow key={i + '1'}>
            <TableCell
              className={cellClasses.root}
              component="th"
              scope="row"
            >{i + 1}</TableCell>
            {/* TODO: when edit button is clicked checkbox column will be displayed */}
            {/*<TableCell className={cellClasses.root}>e</TableCell>*/}
            <TableCell className={cellClasses.root}>{visitorRow.name}</TableCell>
            <TableCell className={cellClasses.root}>{visitorRow.email}</TableCell>
          </TableRow>
        ))}
      </TableBody>
    </Table>
  )
}

export const OfficeVisits: React.FC = () => {
  const { apiClient } = useServices();

  const [startDate, setStartDate] = useState('2020-07-01');
  const [endDate, setEndDate] = useState('2020-07-14');
  const [site, setSite] = useState('Stuttgart');

  const officeBookingsRes = useQuery(
    officeBookingsQueryKey(site, startDate, endDate),
    () => apiClient.getOfficeBookingsInfo(site, startDate, endDate)
  );

  return (
    <>
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
          <Button
            color="primary"
            variant="outlined"
          >
            Export list
          </Button>
        </ToolbarItem>
      </Toolbar>
      <RenderQuery
        query={combineQueries({
          officeBookings: officeBookingsRes
        })}
        onLoading={(data, children) => children(data || ({} as any), true)}
        onError={(error, children) => children({} as any, false, error)}
      >
        {({ officeBookings }, isLoading: boolean, error?: Error) => {
          console.log('officeBookings', officeBookings);
          console.log('isLoading', isLoading);
          console.log('error', error);
          // TODO: unmock api
          // TODO: count booked amount

          return (
            <CollapsibleTable
              childComponent={Visitors}
              childTableHead={childTableHead}
              parentTableHead={parentTableHead}
              rows={rows}
            />
          )
        }}
      </RenderQuery>
    </>
  );
}
