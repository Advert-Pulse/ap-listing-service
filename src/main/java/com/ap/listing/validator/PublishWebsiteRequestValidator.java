package com.ap.listing.validator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishWebsiteRequestValidator
 */

import com.ap.listing.dao.repository.WebsiteCategoryRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.bloggios.provider.utils.WordsCounter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class PublishWebsiteRequestValidator {

    private final WebsiteCategoryRepository websiteCategoryRepository;

    public void validate(PublishWebsiteRequest publishWebsiteRequest) {
        categoryValidator(publishWebsiteRequest.getCategories());
        specialRequirementsValidator(publishWebsiteRequest.getSpecialRequirements());
        productSpecialRequirementsValidator(publishWebsiteRequest.getProductSpecialRequirements());
        basicContentSizeValidator(publishWebsiteRequest.getBasicContentSize());
        linkAttributeValidator(publishWebsiteRequest.getLinkAttribute());
        validatePricing(publishWebsiteRequest.getContentPlacementPrice(), "Content Placement");
        validatePricing(publishWebsiteRequest.getWritingAndPlacementPrice(), "Writing and Placement");
        validatePricing(publishWebsiteRequest.getExtraSizeContentWriting(), "Extra Size Content");
        validatePricing(publishWebsiteRequest.getSpecialTopicPricing(), "Special Topic");
        validatePricing(publishWebsiteRequest.getExtraLinkPricing(), "Extra Link");
        validatePricing(publishWebsiteRequest.getLinkInsertionPrice(), "Link Insertion");
        validatePricing(publishWebsiteRequest.getSpecialTopicPricing(), "Special Topic");
        validateUrlList(publishWebsiteRequest.getBestArticleLinkForGuestPosting(), "Best Article Link for Guest Posting");
        validateUrlList(publishWebsiteRequest.getBestArticleLinkForLinkInsertion(), "Best Article Link for Link Insertion");
    }

    private void categoryValidator(List<String> categories) throws BadRequestException {
        if (CollectionUtils.isEmpty(categories)) {
            throw new BadRequestException(ErrorData.CATEGORY_MANDATORY);
        }
        List<String> filteredCategories = categories
                .stream()
                .filter(Objects::nonNull)
                .map(category -> category.toLowerCase().trim())
                .toList();
        if (filteredCategories.size() > 4) {
            throw new BadRequestException(ErrorData.CATEGORY_LIMIT_FOUR);
        }
        List<WebsiteCategory> allWebsiteCategories = websiteCategoryRepository.findAll();
        List<String> storedWebsiteCategory = allWebsiteCategories
                .stream()
                .map(WebsiteCategory::getCategory)
                .toList();
        Map<Boolean, List<String>> partitionedList = filteredCategories
                .parallelStream()
                .collect(Collectors.partitioningBy(storedWebsiteCategory::contains));
        List<String> notPresentCategories = partitionedList.get(Boolean.FALSE);
        if (!CollectionUtils.isEmpty(notPresentCategories)) {
            throw new BadRequestException(
                    ErrorData.CATEGORIES_INVALID,
                    String.format("%s categories are invalid", String.join(", ", notPresentCategories))
            );
        }
    }

    private void specialRequirementsValidator(String specialRequirements) {
        if (Objects.nonNull(specialRequirements)) {
            if (specialRequirements.length() > 7000) {
                throw new BadRequestException(ErrorData.SPECIAL_REQUIREMENT_LENGTH_EXCEED);
            }

            int words = WordsCounter.countWords(specialRequirements);
            if (words > 400) {
                throw new BadRequestException(ErrorData.SPECIAL_REQUIREMENT_WORD_EXCEED);
            }
        }
    }

    private void productSpecialRequirementsValidator(String productSpecialRequirements) {
        if (Objects.nonNull(productSpecialRequirements)) {
            if (productSpecialRequirements.length() > 7000) {
                throw new BadRequestException(ErrorData.PRODUCT_SPECIAL_REQUIREMENT_LENGTH_EXCEED);
            }

            int words = WordsCounter.countWords(productSpecialRequirements);
            if (words > 400) {
                throw new BadRequestException(ErrorData.PRODUCT_SPECIAL_REQUIREMENT_WORD_EXCEED);
            }
        }
    }

    private void basicContentSizeValidator(String basicContentSize) {
        if (Objects.isNull(basicContentSize)) {
            throw new BadRequestException(ErrorData.BASIC_CONTENT_SIZE_MANDATORY);
        }
        List<String> sizes = List.of("500", "1000", "1500", "2000+");
        if (!sizes.contains(basicContentSize)) {
            throw new BadRequestException(ErrorData.INVALID_BASIC_CONTENT_SIZE);
        }
    }

    private void linkAttributeValidator(String linkAttribute) {
        if (Objects.isNull(linkAttribute)) {
            throw new BadRequestException(ErrorData.LINK_ATTRIBUTE_MANDATORY);
        }
        List<String> linkAttributes = List.of("dofollow", "nofollow");
        if (!linkAttributes.contains(linkAttribute.toLowerCase())) {
            throw new BadRequestException(ErrorData.LINK_ATTRIBUTE_INVALID);
        }
    }

    private void validatePricing(Double field, String fieldName) {
        if (Objects.nonNull(field) && field < 4) {
            throw new BadRequestException(ErrorData.PRICING_INVALID, String.format("%s Price should be more than USD 4", fieldName));
        }
    }

    private void validateUrlList(List<String> urlList, String fieldName) {
        if (!CollectionUtils.isEmpty(urlList)) {
            if (urlList.size() > 3) {
                throw new BadRequestException(ErrorData.URL_LIST_SIZE_LENGTH_EXCEED, String.format("%s URLs should be less than 3 ", fieldName));
            }
            urlList
                    .forEach(e -> {
                        try {
                            new URL(e);
                        } catch (MalformedURLException ignored) {
                            throw new BadRequestException(ErrorData.INVALID_LIST_URL, String.format("One of the URL is invalid for %s", fieldName));
                        }
                    });
        }
    }

}
