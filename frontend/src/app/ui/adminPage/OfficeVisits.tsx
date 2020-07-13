import React from 'react';
import {
  Table,
  TableBody,
  TableCell,
  TableRow,
  TableHead
} from '@material-ui/core';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';


const childTableHead: ICollapsibleTableHead[] = [
  {
    title: 'nr'
  },
  {
    title: ''
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
    title: 'Date'
  },
  {
    title: 'Office'
  },
  {
    title: 'Capacity utilisation'
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


const Visitors = ({ row, head }: { row: ReturnType<typeof createData>, head: ICollapsibleTableHead[] }) => {
  return (
    <Table size="small" aria-label="visitors">
      <TableHead>
        <TableRow>
          {
            head.map(({ align = 'left', title } : ICollapsibleTableHead) =>
              <TableCell align={align}>{title}</TableCell>
            )}
        </TableRow>
      </TableHead>
      <TableBody>
        {row.visitors.map((visitorRow, i) => (
          <TableRow key={i + 1}>
            <TableCell>{i + 1}</TableCell>
            <TableCell>empty</TableCell>
            <TableCell>{visitorRow.name}</TableCell>
            <TableCell>{visitorRow.email}</TableCell>
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
