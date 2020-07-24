import React, { useMemo, useState } from 'react';
import { useQuery } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { combineQueries, officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { ICapacityDto, IPerson } from '../../services/apiClientService';
import { ICapacityDtoMapped, IOverviewTable, IPersonMapped } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { VisitorsToolbar } from './VisitorsToolbar';
import { BookingsTable } from './BookingsTable';


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
  const today = dayjs().utc().startOf('day');

  const [startDate, setStartDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [endDate, setEndDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [currentSite, setCurrentSite] = React.useState((offices && offices[2].site) || '');
  const [currentUser, setCurrentUser] = React.useState('');
  const [range, setRange] = React.useState(0);

  const startDateStr = startDate.format('YYYY-MM-DD');
  const endDateStr = endDate.format('YYYY-MM-DD');

  const handleSearch = () => {
    // TODO: @egor implement api call
    // due to range we need to swap startDate with endDate for the api call
    console.log('api call with params',
      currentUser,
      startDate.subtract(range, 'day').format('YYYY-MM-DD'),
      startDate.format('YYYY-MM-DD')
    );
  }

  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const handleUserChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentUser(event.target.value as string);
  }

  const handleRangeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setRange(event.target.value as number);
  }

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  }

  const officeBookingsRes = useQuery(
    officeBookingsQueryKey(currentSite, startDateStr, endDateStr),
    () => apiClient.getOfficeBookings(currentSite, startDateStr, endDateStr)
  );

  const rows: ICapacityDtoMapped[] = useMemo(() => {
    const { data } = officeBookingsRes;
    const mappedBookings: ICapacityDtoMapped[] | undefined = data && data.map(
      (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: currentSite }));

    return mappedBookings || [];
  }, [officeBookingsRes]);

  return (
    <>
      {
        !isTracking ? (
          <VisitorsToolbar
            tableData={rows}
            startDate={startDate}
            endDate={endDate}
            offices={offices}
            currentSite={currentSite}
            onDateChange={handleDateChange}
            onSiteChange={handleSiteChange}
            onSearch={handleSearch}
          />
        ): (
          <TrackingToolbar
            tableData={rows}
            startDate={startDate}
            range={range}
            users={users}
            currentUser={currentUser}
            onDateChange={handleDateChange}
            onUserChange={handleUserChange}
            onRangeChange={handleRangeChange}
            onSearch={handleSearch}
          />
        )
      }
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
          // TODO @egor: empty and load table state

          return (
            <CollapsibleTable
              childComponent={BookingsTable}
              childTableHead={childTableHead}
              parentTableHead={parentTableHead}
              empty={'No results.'}
              rows={rows}
            />
          )
        }}
      </RenderQuery>
    </>
  );
}
