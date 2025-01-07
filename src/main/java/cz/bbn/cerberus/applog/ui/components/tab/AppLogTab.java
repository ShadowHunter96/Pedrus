package cz.bbn.cerberus.applog.ui.components.tab;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.dto.AppLogFilterDto;
import cz.bbn.cerberus.applog.ui.components.AppLogFilterComponent;
import cz.bbn.cerberus.applog.ui.components.AppLogGridComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.user.UserService;
import org.springframework.data.domain.Sort;

public class AppLogTab extends TabSimpleComponent {

    public static final int TAB_INDEX = 0;

    private final AppEnv appEnv;
    private final AppLogService appLogService;
    private final UserService userService;

    public AppLogTab(AppEnv appEnv,
                     AppLogService appLogService, UserService userService) {
        this.appEnv = appEnv;
        this.appLogService = appLogService;
        this.userService = userService;
        initView();
    }

    private void initView() {
        removeAll();
        setSizeFull();
        this.setId(RobotFrameworkVariables.APP_LOG_TAB_ID.getValue());

        Button search = VaadinComponents.getSearchButton();

        AppLogFilterComponent appLogFilterComponent = new AppLogFilterComponent(search,
                getListActionActionList(), userService);
        this.add(appLogFilterComponent);

        AppLogGridComponent appLogGridComponent =
                new AppLogGridComponent(appEnv, getItemsAction(appLogFilterComponent));

        this.add(appLogGridComponent);
        appLogGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> appLogGridComponent.loadData());
    }

    private ItemsAction<AppLogDto> getItemsAction(AppLogFilterComponent filterComponent) {
        return (query, orderList) -> {
            AppLogFilterDto filterDto = filterComponent.getAppLogFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("date"));
            }
            filterDto.setOrderList(orderList);
            return appLogService.findAppLogDtoPage(filterDto);
        };
    }

    private ListAction<ItemDto> getListActionActionList() {
        return id -> appLogService.getActionList();
    }

}
