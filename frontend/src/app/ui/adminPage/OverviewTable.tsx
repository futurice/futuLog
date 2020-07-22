import React, { useMemo, useState } from 'react';
import { useQuery } from 'react-query';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { combineQueries, officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { TableToolbar } from './Toolbar';
import { VisitorsTable } from './VisitorsTable';
import { ICapacityDto, IPerson } from '../../services/apiClientService';


interface IOverviewTable {
  isTracking?: boolean,
  users?: string[],
  offices?: string[]
}

export interface ICapacityDtoMapped {
  date: string;
  site: string;
  visitors: IPersonMapped[];
  utilisation: number;
}

export interface IPersonMapped {
  name: string;
  email: string;
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

export function mapBookingsForUI({
  bookings,
  site
}: {
  bookings: ICapacityDto,
  site: string
}): ICapacityDtoMapped {
  const { people, date } = bookings;
  const mappedPeople: IPersonMapped[] = people.map((person: IPerson) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email
  }));

  return {
    date: date,
    site,
    visitors: mappedPeople,
    utilisation: mappedPeople.length
  };
}

export function OverviewTable({
  isTracking,
  users,
  offices
}: IOverviewTable) {
  const { apiClient } = useServices();
  const [startDate, setStartDate] = useState('2020-07-01');
  const [endDate, setEndDate] = useState('2020-07-24');
  const [site, setSite] = useState('Stuttgart');

  // TODO @egor: define event handler for changing a user, dates, range, office. Also check that you do need them here or inside of TableToolbar
  // TODO @egor: pass user, dates, range, office accordingly to TableToolbar component

  const officeBookingsRes = useQuery(
    officeBookingsQueryKey(site, startDate, endDate),
    () => apiClient.getOfficeBookings(site, startDate, endDate)
  );

  const rows: ICapacityDtoMapped[] = useMemo(() => {
    const { data } = officeBookingsRes;
    const mappedBookings: ICapacityDtoMapped[] | undefined = data && data.map(
      (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site }));

    return mappedBookings || [];
  }, [officeBookingsRes]);

  return (
    <>
      {isTracking && (
        <TableToolbar
          startDate={startDate}
          offices={offices}
          users={users}
        />
      )}
      {!isTracking && (
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
          // TODO @egor: count booked amount

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
