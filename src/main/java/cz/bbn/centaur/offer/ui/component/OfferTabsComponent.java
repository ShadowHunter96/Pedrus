package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class OfferTabsComponent extends TabsComponent<OfferDto> {

    public OfferTabsComponent(String title, List<TabEntry> tabEntryList,
                              EntityNewComponentOperation entityNewComponentOperation,
                              Button addSign, boolean isShowSubmitButton, OfferDto dto) {
        super(title, tabEntryList, entityNewComponentOperation);

        if (SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.OFFER_AREA_TECHNOLOGY_SIGN_EDIT.name())) {
            addToButtonFooter(addSign);
        }
        this.addBackButton();
        if (isShowSubmitButton) {
            addSaveButton();
        }
    }
}
