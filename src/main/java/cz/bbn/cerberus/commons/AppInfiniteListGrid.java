package cz.bbn.cerberus.commons;

import com.vaadin.flow.data.provider.ListDataProvider;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class AppInfiniteListGrid<T> extends AppInfiniteGrid<T> {

    private final ListAction<T> listAction;

    protected AppInfiniteListGrid(AppEnv appEnv, ListAction<T> listAction) {
        super(appEnv, null);
        this.listAction = listAction;
    }

    @Override
    public void loadData() {
        setItems(new ListDataProvider<>(listAction.getList(null)));
    }
}
