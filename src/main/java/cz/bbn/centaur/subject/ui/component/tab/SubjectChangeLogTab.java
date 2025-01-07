package cz.bbn.cerberus.subject.ui.component.tab;

import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.ui.components.AppLogGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;

public class SubjectChangeLogTab extends TabSimpleComponent {

    public SubjectChangeLogTab(ItemsAction<AppLogDto> itemsAction, AppEnv appEnv) {
        setSizeFull();
        AppLogGridComponent appLogGridComponent = new AppLogGridComponent(appEnv, itemsAction);
        this.add(appLogGridComponent);
        appLogGridComponent.loadData();
    }
}
