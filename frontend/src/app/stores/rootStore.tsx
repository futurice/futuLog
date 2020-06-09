import { combineReducers, StateFromReducersMapObject } from "@reduxjs/toolkit";
import { useSelector as _useSelector, useDispatch } from "react-redux";
import { remoteStore } from "app/stores/remoteStore";

const reducers = {
  [remoteStore.name]: remoteStore.reducer,
};

export type IRootStore = StateFromReducersMapObject<typeof reducers>;

export const rootStore = combineReducers(reducers);

//
// Pre-typed react-redux hooks for `IRootStore`

export function useSelector<TSelected = unknown>(
  selector: (state: IRootStore) => TSelected,
  equalityFn?: ((left: TSelected, right: TSelected) => boolean) | undefined
): TSelected {
  return _useSelector(selector, equalityFn);
}

// Export this for symmetry
export { useDispatch };
