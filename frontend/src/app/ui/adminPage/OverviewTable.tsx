import React, { useMemo, useState } from 'react';
import { useQuery } from 'react-query';

import CollapsibleTable, { ICollapsibleTableHead } from './CollapsibleTable';
import { combineQueries, officeBookingsQueryKey, RenderQuery } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import { ICapacityDto, IPerson } from '../../services/apiClientService';
import { ICapacityDtoMapped, IOverviewTable, IPersonMapped, ISelectOptionNumber, ISelectOptionString } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { VisitorsToolbar } from './VisitorsToolbar';
import { VisitorsTable } from './VisitorsTable';


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
  const [currentSite, setCurrentSite] = React.useState('');
  const [currentUser, setCurrentUser] = React.useState('');
  const [range, setRange] = React.useState(0);

  const handleSearch = () => {
    console.log('click');
  }

  const handleSiteChange = (newSite: ISelectOptionString) => {
    setCurrentSite(newSite.value);
  }

  const handleUserChange = (newUser: ISelectOptionString) => {
    setCurrentUser(newUser.value);
  }

  const handleRangeChange = (newRange: ISelectOptionNumber) => {
    setRange(newRange.value);
    // TODO: calculate startDate + newRange and update endDate
    // setEndDate();
  }

  const handleDateChange = (newDate: ISelectOptionString, type: string) => {
    if (type === 'start') {
      setStartDate(newDate.value);
    } else {
      setEndDate(newDate.value);
    }
  }

  const officeBookingsRes = useQuery(
    officeBookingsQueryKey(currentSite, startDate, endDate),
    () => apiClient.getOfficeBookings(currentSite, startDate, endDate)
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
        isTracking ? (
          <TrackingToolbar
            tableData={[]}
            startDate={startDate}
            range={range}
            users={users}
            currentUser={currentUser}
            onDateChange={handleDateChange}
            onUserChange={handleUserChange}
            onRangeChange={handleRangeChange}
            onSearch={handleSearch}
          />
        ) : (
          <VisitorsToolbar
            tableData={[]}
            startDate={startDate}
            endDate={endDate}
            offices={offices}
            currentSite={currentSite}
            onSiteChange={handleSiteChange}
            onDateChange={handleDateChange}
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
              childComponent={VisitorsTable}
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
