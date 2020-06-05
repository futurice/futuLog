import { combineReducers, StateFromReducersMapObject } from "@reduxjs/toolkit";
import { userStore } from "app/stores/userStore";
import { useSelector as _useSelector, useDispatch } from "react-redux";

const reducers = {
  [userStore.name]: userStore.reducer,
};

export type IRootStore = StateFromReducersMapObject<typeof reducers>;

export const rootStore = combineReducers(reducers);

//
// Pre-typed react-redux hooks

export function useSelector<TSelected = unknown>(
  selector: (state: IRootStore) => TSelected,
  equalityFn?: ((left: TSelected, right: TSelected) => boolean) | undefined
): TSelected {
  return _useSelector(selector, equalityFn);
}

// Export this for symmetry
export { useDispatch };
