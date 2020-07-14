import React from 'react';
import {
  Table,
  TableBody,
  TableCell,
  TableRow,
  TableHead
} from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { colors } from '../ux/theme';


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

const Visitors = ({ row, head }: { row: ReturnType<typeof createData>, head: ICollapsibleTableHead[] }) => {
  const tableClasses = useChildTableStyles();
  const cellClasses = useChildCellStyles();
  const headCellClasses = useTableHeadCellStyles();

  return (
    <Table className={tableClasses.root} size="small" aria-label="visitors">
      <TableHead>
        <TableRow>
          {
            head.map(({ align = 'left', title } : ICollapsibleTableHead) =>
              <TableCell className={headCellClasses.root} align={align}>{title}</TableCell>
            )}
        </TableRow>
      </TableHead>
      <TableBody>
        {row.visitors.map((visitorRow, i) => (
          <TableRow key={i + '1'}>
            <TableCell className={cellClasses.root} component="th" scope="row">{i + 1}</TableCell>
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
  return (
    <CollapsibleTable
      childComponent={Visitors}
      childTableHead={childTableHead}
      parentTableHead={parentTableHead}
      rows={rows}
    />
  );
}
