import React, { useMemo, useState } from 'react';
import { queryCache, useMutation, useQuery } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { userBookingsQueryKey } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import {
  IOfficeSpaceDto,
  IUserBookingsDto,
  IUserBookingsRequestDto,
  IUserDto
} from '../../services/apiClientService';
import { ITableDataDto, ICollapsibleTableHead } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { BookingsTable } from './BookingsTable';
import { CenteredSpinner, CenteredSpinnerContainer } from '../ux/spinner';


interface IPersonTrackingPanel {
  users?: IUserDto[];
  offices?: IOfficeSpaceDto[];
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
  date,
  site,
  userEmail
}: {
  date: string,
  site: string,
  userEmail: string
}): ITableDataDto {
  // const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
  //   name: `${person.first_name} ${person.last_name}`,
  //   email: person.email
  // }));

  // [
  //   {
  //     "userEmail": "string",
  //     "site": "string",
  //     "date": "2016-07-22",
  //     "workmode": {
  //       "type": "string",
  //       "confirmed": true,
  //       "name": "string"
  //     }
  //   }
  // ]

  return {
    date: date,
    site,
    visitors: [] as any,
    utilisation: 0
  };
}

export function PersonTrackingPanel({
  users,
  offices
}: IPersonTrackingPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf('day');

  const [startDate, setStartDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [endDate, setEndDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [currentUser, setCurrentUser] = React.useState('');
  const [range, setRange] = React.useState(0);

  const startDateStr = startDate.format('YYYY-MM-DD');
  const endDateStr = endDate.format('YYYY-MM-DD');

  const handleSearch = (type: string) => {
    // due to range we need to swap startDate with endDate for the api call
    mutateUserBookings({
      user: currentUser,
      startDate: startDate.subtract(range, 'day').format('YYYY-MM-DD'),
      endDate: startDate.format('YYYY-MM-DD')
    });
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

  const userBookingsRes = useQuery(
    userBookingsQueryKey(currentUser, startDateStr, endDateStr),
    () => apiClient.getUserBookings({ user: currentUser, startDate: startDateStr, endDate: endDateStr })
  );

  const [mutateUserBookings, mutateUserBookingsRes] = useMutation(
    ({ user, startDate, endDate}: IUserBookingsRequestDto) => apiClient.getUserBookings({ user, startDate, endDate}),
    {
      onSuccess: () => queryCache.refetchQueries(userBookingsQueryKey(currentUser, startDateStr, endDateStr)),
    }
  );

  const rows: ITableDataDto[] = useMemo(() => {
    const { data } = userBookingsRes;
    // const mappedBookings: ITableDataDto[] | undefined = data && data.map(
    //   (item: IUserBookingsDto) => mapBookingsForUI(item));

    return [];
  }, [userBookingsRes]);

  return (
    <>
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
      {
        mutateUserBookingsRes.status === 'loading' ? (
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
      }
    </>
  );
}
