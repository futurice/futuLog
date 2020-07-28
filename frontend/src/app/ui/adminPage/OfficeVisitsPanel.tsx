import React, { useMemo, useState } from 'react';
import { queryCache, useMutation } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { officeBookingsQueryKey } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import {
  ICapacityDto,
  IOfficeBookingsRequestDto,
  IOfficeSpaceDto,
  IUserDto
} from '../../services/apiClientService';
import { ITableDataDto, ICollapsibleTableHead } from './types';
import { VisitorsToolbar } from './VisitorsToolbar';
import { BookingsTable } from './BookingsTable';
import { CenteredSpinner, CenteredSpinnerContainer } from '../ux/spinner';
import { mapBookingsForUI } from './common';

interface IOfficeVisitsPanel {
  users?: IUserDto[];
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

export function OfficeVisitsPanel({
  offices
}: IOfficeVisitsPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf('day');

  const [startDate, setStartDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [endDate, setEndDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [currentSite, setCurrentSite] = React.useState((offices && offices[2].site) || '');

  const startDateStr = startDate.format('YYYY-MM-DD');
  const endDateStr = endDate.format('YYYY-MM-DD');

  const handleSearch = () => {
    mutateOfficeBookings({
      site: currentSite,
      startDate: startDate.format('YYYY-MM-DD'),
      endDate: endDate.format('YYYY-MM-DD')
    });
  }

  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  }

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    ({ site, startDate, endDate}: IOfficeBookingsRequestDto) => apiClient.getOfficeBookings({ site, startDate, endDate}),
    {
      onSuccess: () => queryCache.refetchQueries(officeBookingsQueryKey(currentSite, startDateStr, endDateStr)),
    }
  );

  const rows: ITableDataDto[] = useMemo(() => {
    const { data } = mutateOfficeBookingsRes;
    const mappedBookings: ITableDataDto[] | undefined = data && data.map(
      (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: currentSite }));

    return mappedBookings || [];
  }, [mutateOfficeBookingsRes]);

  return (
    <>
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
      {
        mutateOfficeBookingsRes.status === 'loading' ? (
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
