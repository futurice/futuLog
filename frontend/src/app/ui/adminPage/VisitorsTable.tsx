import React from 'react';
import { Table, TableBody, TableCell, TableHead, TableRow } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';

import { createData } from './OverviewTable';
import { colors } from '../ux/theme';
import { ICollapsibleTableHead } from './CollapsibleTable';


interface IVisitorsTable {
  row: ReturnType<typeof createData>,
  head: ICollapsibleTableHead[]
}

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


export function VisitorsTable ({ row, head }: IVisitorsTable) {
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
