package cz.bbn.cerberus.enumeration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class EnumerationTabComponent extends TabSimpleComponent {

    private final int tabIndex;
    private final AppEnv appEnv;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final EnumerationTypeDto enumerationTypeDto;

    private EnumerationGridComponent enumerationGridComponent;

    public EnumerationTabComponent(int tabIndex, AppEnv appEnv,
                                   EnumerationComponentOperation enumerationComponentOperation,
                                   EnumerationTypeDto enumerationTypeDto) {
        this.tabIndex = tabIndex;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.appEnv = appEnv;
        this.enumerationTypeDto = enumerationTypeDto;
        initComponent();
    }

    private void initComponent() {
        Button search = VaadinComponents.getSearchButton();
        EnumerationFilterComponent enumerationFilterComponent =
                new EnumerationFilterComponent(search, enumerationTypeDto);
        this.add(enumerationFilterComponent);

        enumerationGridComponent =
                new EnumerationGridComponent(enumerationComponentOperation.getDeleteAction(), appEnv,
                        enumerationComponentOperation.getItemsAction(
                                enumerationFilterComponent, enumerationTypeDto.getId()), true, tabIndex);
        enumerationGridComponent.setSizeFull();
        enumerationGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> enumerationGridComponent.loadData());
        this.add(enumerationGridComponent);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        enumerationGridComponent.loadData();
    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.valueOf(enumerationTypeDto.getPermissionKey().concat("_EDIT")))) {
            Button addNew = VaadinComponents.getNewButton(
                    Transl.get("Add ".concat(enumerationTypeDto.getTranslationKey())));
            addNew.addClickListener(e -> new EnumerationNewDialog(
                    enumerationComponentOperation, enumerationGridComponent,
                    enumerationTypeDto, appEnv, tabIndex).open());
            return addNew;
        }
        return null;
    }
}
