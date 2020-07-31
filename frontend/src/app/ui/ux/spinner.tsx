import React from 'react';
import { makeStyles, styled } from '@material-ui/core/styles';

import { Spinner } from './icons';

const useSpinnerAnimationStyles = makeStyles({
  transitionGroup: {
    display: 'flex',
    animation: '$rotation 1s linear infinite',
  },

  '@keyframes rotation': {
    from: {
      transform: 'rotate(0deg)',
    },
    to: {
      transform: 'rotate(360deg)',
    }
  },
});

export const SpinnerContainer = styled('div')({
  position: 'absolute',
  top: '50%',
  left: '50%',
  transform: 'translateY(-50%)'
});

export const CenteredSpinnerContainer = styled('div')({
  height: '120px',
  width: '100%',
  position: 'relative'
});

export const CenteredSpinner = (): JSX.Element => {
  const classes = useSpinnerAnimationStyles();

  return (
    <SpinnerContainer>
      <div className={classes.transitionGroup}>
        <Spinner/>
      </div>
    </SpinnerContainer>
  )
};
