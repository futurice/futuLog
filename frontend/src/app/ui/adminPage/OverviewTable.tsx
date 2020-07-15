import React, { useState } from 'react';
import { useQuery } from 'react-query';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { combineQueries, officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { TableToolbar } from './Toolbar';
import { VisitorsTable } from './VisitorsTable';


interface IOverviewTable {
  isTracking?: boolean,
  users?: string[],
  offices?: string[]
}

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


export function OverviewTable ({
  isTracking,
  users,
  offices
}: IOverviewTable) {
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
      { isTracking && (
        <TableToolbar
          startDate={startDate}
          offices={offices}
          users={users}
        />
      )}
      { !isTracking && (
        <TableToolbar
          startDate={startDate}
          endDate={endDate}
          offices={offices}
        />
      )}
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
              childComponent={VisitorsTable}
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
