/*
* TODO WIP: currently api is not ready to return table, it handles single site for the range but we need
* TODO WIP: to be able to send user and dates and get a response in the following
* TODO WIP: format [{ date: '', people: []}], like it's done in OfficeVisitsPanel.tsx
* */

import React, { useMemo, useState } from 'react';
import { queryCache, useMutation, useQuery } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { officeBookingsQueryKey, userBookingsQueryKey } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import {
  ICapacityDto,
  IOfficeBookingsRequestDto,
  IOfficeSpaceDto,
  IUserBookingsRequestDto,
  IUserDto
} from '../../services/apiClientService';
import { ICollapsibleTableHead, ITableDataDto } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { BookingsTable } from './BookingsTable';
import { CenteredSpinner, CenteredSpinnerContainer } from '../ux/spinner';
import { mapBookingsForUI } from './common';


interface IPersonTrackingPanel {
  usersa?: IUserDto[];
  offices?: IOfficeSpaceDto[];
}


const childTableHead: ICollapsibleTableHead[] = [
  {
    title: 'nr.'
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
    width: '10%'
  },
  {
    title: 'Office',
    width: '10%'
  },
  {
    title: 'Person',
    width: '15%'
  },
  {
    title: 'Capacity utilisation',
    width: '10%'
  },
  {
    title: '',
    width: '50%'
  }
];

// TODO: remove users constant, rename usersa -> users
// TODO: handle on search click
// TODO: adjust rows and its types/interface
export function PersonTrackingPanel({
  usersa
}: IPersonTrackingPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf('day');

  const users = [{
      "email": "jan.brugge@futurice.com",
      "portrait_full_url": "https://api.fum.futurice.com/media/portraits/full/jvan20200721174329.jpeg",
      "portrait_thumb_url": "https://api.fum.futurice.com/media/portraits/thumb/jvan20200721174329_175_0_2795_3935.jpeg",
      "first_name": "E",
      "portrait_badge_url": "https://api.fum.futurice.com/media/portraits/badge/jvan20200721174329_175_0_2795_3935.png",
      "last_name": "Egge",
      "isAdmin": true
  },
    {
      "email": "van.brugge@futurice.com",
      "portrait_full_url": "https://api.fum.futurice.com/media/portraits/full/jvan20200721174329.jpeg",
      "portrait_thumb_url": "https://api.fum.futurice.com/media/portraits/thumb/jvan20200721174329_175_0_2795_3935.jpeg",
      "first_name": "Jean",
      "portrait_badge_url": "https://api.fum.futurice.com/media/portraits/badge/jvan20200721174329_175_0_2795_3935.png",
      "last_name": "Veee",
      "isAdmin": true
    },
    {
      "email": "jan.van.brugge@futurice.com",
      "portrait_full_url": "https://api.fum.futurice.com/media/portraits/full/jvan20200721174329.jpeg",
      "portrait_thumb_url": "https://api.fum.futurice.com/media/portraits/thumb/jvan20200721174329_175_0_2795_3935.jpeg",
      "first_name": "Jan",
      "portrait_badge_url": "https://api.fum.futurice.com/media/portraits/badge/jvan20200721174329_175_0_2795_3935.png",
      "last_name": "van Brügge",
      "isAdmin": true
    }
    ];
  const [startDate, setStartDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [endDate, setEndDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [currentUser, setCurrentUser] = React.useState('');
  const [range, setRange] = React.useState(0);

  const startDateStr = startDate.format('YYYY-MM-DD');
  const endDateStr = endDate.format('YYYY-MM-DD');

  const handleSearch = () => {
    // due to range we need to swap startDate with endDate for the api call
    // user data comes as an array with date, name, email, site,
    // for every entry we need to request officeBooking that takes as a param site === user.site
    // and endDate === user.date and startDate === user.date

    mutateUserBookingsRes.data?.forEach(() => {

    });
  }

  const handleUserChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    const userEmail = event.target.value as string;
    setCurrentUser(userEmail);

    mutateUserBookings({
      // site: currentUser.site,
      user: userEmail,
      startDate: startDate.subtract(range, 'day').format('YYYY-MM-DD'),
      endDate: startDate.format('YYYY-MM-DD')
    });

    // mutateOfficeBookings({
    //   site: currentSite,
    //   startDate: startDate.format('YYYY-MM-DD'),
    //   endDate: endDate.format('YYYY-MM-DD')
    // });
  }

  const handleRangeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setRange(event.target.value as number);
  }

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  }

  // Fetch user
  const userBookingsRes = useQuery(
    userBookingsQueryKey(currentUser, startDateStr, endDateStr),
    () => apiClient.getUserBookings({ user: currentUser, startDate: startDateStr, endDate: endDateStr })
  );

  // Fetch all offices where fetched user was
  // TODO unmock currenSite
  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
      ({ site, startDate, endDate}: IOfficeBookingsRequestDto) => apiClient.getOfficeBookings({ site, startDate, endDate}),
      {
        onSuccess: () => queryCache.refetchQueries(officeBookingsQueryKey('Stuttgart', startDateStr, endDateStr)),
      }
  );

  const [mutateUserBookings, mutateUserBookingsRes] = useMutation(
    ({ user, startDate, endDate}: IUserBookingsRequestDto) => apiClient.getUserBookings({ user, startDate, endDate}),
    {
      onSuccess: () => queryCache.refetchQueries(userBookingsQueryKey(currentUser, startDateStr, endDateStr)),
    }
  );

  const rows: ITableDataDto[] = useMemo(() => {
    const { data } = mutateOfficeBookingsRes;
    const mappedBookings: ITableDataDto[] | undefined = data && data.map(
      // (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: currentUser.site }));
      (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: 'Berlin' }));

    return mappedBookings || [];
  }, [mutateOfficeBookingsRes]);

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
