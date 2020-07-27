import React, { useMemo, useState } from 'react';
import { queryCache, useMutation, useQuery } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { ICapacityDto, IOfficeBookingsRequestDto, IUserDto } from '../../services/apiClientService';
import { ICapacityDtoMapped, ICollapsibleTableHead, IOverviewTable, IUserDtoMapped } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { VisitorsToolbar } from './VisitorsToolbar';
import { BookingsTable } from './BookingsTable';
import { CenteredSpinner, CenteredSpinnerContainer } from '../ux/spinner';
import { H2Center } from '../ux/text';


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
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
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

  const handleSearch = (type: string) => {
    // TODO: @egor implement api call
    // due to range we need to swap startDate with endDate for the api call
    if (type === 'user') {

    } else {
      mutateOfficeBookings({
        site: currentSite,
        startDate: startDate.subtract(range, 'day').format('YYYY-MM-DD'),
        endDate: startDate.format('YYYY-MM-DD')
      });
    }
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
    () => apiClient.getOfficeBookings({ site: currentSite, startDate: startDateStr, endDate: endDateStr })
  );

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    ({ site, startDate, endDate}: IOfficeBookingsRequestDto) => apiClient.getOfficeBookings({ site, startDate, endDate}),
    {
      onSuccess: () => queryCache.refetchQueries(officeBookingsQueryKey(currentSite, startDateStr, endDateStr)),
    }
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
        query={officeBookingsRes}
        onError={(error) => <H2Center>{error.message}</H2Center>}
        onLoading={(data, children) => children(data || [])}
      >
        {(officeBookings, isLoading: boolean, error?: Error) => {
          console.log('officeBookings', officeBookings);
          console.log('error', error);

          return (
            isLoading ? (
              <CenteredSpinnerContainer style={{ minHeight: '250px' }}>
                <CenteredSpinner />
              </CenteredSpinnerContainer>
            ) : (
              <CollapsibleTable
                childComponent={BookingsTable}
                childTableHead={childTableHead}
                parentTableHead={parentTableHead}
                empty={'No result for the selected parameters.'}
                rows={rows}
              />
            )
          )
        }}
      </RenderQuery>
    </>
  );
}
