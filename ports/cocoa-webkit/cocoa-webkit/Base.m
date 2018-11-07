//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Base.h"
#import "Minibuffer.h"


@implementation Base

@synthesize minibuffer;
@synthesize buffer;
@synthesize minibufferHeightConstraint;

- (instancetype)init
{
    self = [super init];
    [self setOrientation:NSUserInterfaceLayoutOrientationVertical];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setMinibuffer:[[Minibuffer alloc] init]];
    [self setMinibufferHeightConstraint:
     [NSLayoutConstraint constraintWithItem:self.minibuffer
                                  attribute:NSLayoutAttributeHeight
                                  relatedBy:NSLayoutRelationEqual
                                     toItem:nil
                                  attribute:NSLayoutAttributeNotAnAttribute
                                 multiplier:1.0f
                                   constant:10]];

    Buffer *buffer = [[Buffer alloc] init];
    [self setBuffer:buffer];
    
    [[self minibuffer] addConstraint:[self minibufferHeightConstraint]];

    [self addArrangedSubview:buffer];
    [self addArrangedSubview:minibuffer];
    
    return self;
}

- (void)setActiveBuffer:(Buffer*)buffer {
    [self replaceSubview:[self buffer] with:buffer];
}

- (int)setMinibufferHeight:(int)height
{
    [[self minibufferHeightConstraint] setConstant:height];
    return height;
}

- (void)minibufferExecuteJavascript:(NSString *)javascript
{
    [[self minibuffer] stringByEvaluatingJavaScriptFromString:javascript];
}

@end
